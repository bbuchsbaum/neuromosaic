const { chromium } = require('@playwright/test');
const fs = require('node:fs');
const assert = require('node:assert/strict');
const path = require('node:path');
const { pathToFileURL } = require('node:url');
const root=require('node:path').resolve(__dirname, '../e2e/.artifacts/surface-contrast');
(async()=>{
 const browser=await chromium.launch({headless:true});
 const records=[];
 try {
  for(const variant of ['before','continuous','binary']) {
   const page=await browser.newPage({viewport:{width:1400,height:850},deviceScaleFactor:1});
   const errors=[]; page.on('pageerror',e=>errors.push(e.message));
   await page.goto(pathToFileURL(path.join(root,variant+'.html')).href);
   await page.waitForFunction(()=>document.querySelector('.surfwidget')?.__surfviewHandle?.viewer,{timeout:30000});
   await page.evaluate(async()=>await document.querySelector('.surfwidget').__surfviewHandle.ready);
   for(const view of ['lateral','medial']) {
    const state=await page.evaluate(async({view,variant})=>{
     const h=document.querySelector('.surfwidget').__surfviewHandle;
     h.setView(view);
     await new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve)));
     const surfaces=[...h.viewer.surfaces.values()];
     return {
      scientificAssets: Object.values(h.manifest.assets)
        .filter(a=>['vertices','faces','values'].includes(a.role))
        .map(a=>({role:a.role,sha256:a.sha256})),
      limits:h.manifest.layers.statistic.limits,
      threshold:h.manifest.layers.statistic.threshold,
      camera:h.viewer.getCameraState(),
      layers:surfaces.map(s=>s.layerStack.getAllLayers().map(l=>({id:l.id,opacity:l.opacity,visible:l.visible}))),
      anatomy:surfaces.map(s=>{
       const l=s.getCurvatureLayer();
       if(!l) return null;
       const v=l.getRGBAData(s.geometry.vertices.length/3);
       let min=1,max=0;
       for(let i=0;i<v.length;i+=4){min=Math.min(min,v[i]);max=Math.max(max,v[i]);}
       return {min,max};
      }),
      png:h.viewer.exportPNG({width:1400,height:700,colorbar:false,scaleBar:false,transparent:false})
     };
    },{view,variant});
    fs.writeFileSync(path.join(root,`${variant}-${view}-export.png`),Buffer.from(state.png.split(',')[1],'base64'));
    delete state.png;
    await page.screenshot({path:path.join(root,`${variant}-${view}.png`)});
    records.push({variant,view,errors,...state});
   }
   await page.evaluate(()=>document.querySelector('.surfwidget').__surfviewHandle.dispose());
   await page.close();
  }
 } finally {await browser.close();}
 fs.writeFileSync(path.join(root,'browser-evidence.json'),JSON.stringify(records,null,2));
 for(const view of ['lateral','medial']) {
  const variants=records.filter(r=>r.view===view);
  for(const record of variants.slice(1)) {
   assert.deepEqual(record.camera,variants[0].camera,'Camera changed across styles');
   assert.deepEqual(record.scientificAssets,variants[0].scientificAssets,'Scientific data changed');
   assert.deepEqual(record.limits,variants[0].limits);
   assert.deepEqual(record.threshold,variants[0].threshold);
   assert(record.anatomy.every(a=>a.max-a.min>=0.49),'Insufficient anatomy contrast');
  }
 }
 if(records.some(r=>r.errors.length)) throw Error('Browser errors: '+JSON.stringify(records));
 console.log(JSON.stringify(records.map(({variant,view,anatomy})=>({variant,view,anatomy})),null,2));
})().catch(e=>{console.error(e);process.exitCode=1});
