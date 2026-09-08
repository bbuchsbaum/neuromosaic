const {chromium,expect}=require('@playwright/test');
const {pathToFileURL}=require('node:url');
const fs=require('node:fs');
const root=require('node:path').resolve(__dirname, '../e2e/.artifacts/surface-contrast');
(async()=>{
 const browser=await chromium.launch({headless:true});
 try{
  const page=await browser.newPage({viewport:{width:1440,height:1100}});
  const errors=[];page.on('pageerror',e=>errors.push(e.message));
  await page.goto(pathToFileURL(root+'/report.html').href);
  await page.getByRole('radio',{name:'Surface',exact:true}).check();
  const host=page.locator('[data-nm-surface-host]');
  await expect(host.locator('[data-nm-surface-status]')).toContainText('Interactive surface ready',{timeout:30000});
  const state=()=>host.evaluate(n=>{
   const h=n.__nmSurfaceHandle;
   const s=h.controlTarget.getSnapshot();
   const id=s.capabilities.exclusiveMap.displayedLayerId;
   const layer=s.surfaces.flatMap(x=>x.layers).find(l=>l.id===id);
   return {camera:h.viewer.getCameraState(),id,palette:layer.scalarMapping.colorMap.id,
    range:layer.scalarMapping.displayRange.value,threshold:layer.scalarMapping.maskInterval.value,
    opacity:layer.opacity};
  });
  const original=await state();
  await expect(host.getByRole('slider',{name:'Surface opacity'})).toHaveValue('1');
  await host.getByRole('combobox',{name:'Surface colormap'}).selectOption('viridis');
  await host.getByRole('button',{name:'Reset map display'}).click();
  expect((await state()).palette).toBe(original.palette);
  expect((await state()).threshold).toEqual(original.threshold);
  const selector=host.getByRole('combobox',{name:'Displayed surface map'});
  const options=await selector.locator('option').evaluateAll(nodes=>nodes.map(n=>n.value));
  await selector.selectOption(options[1]);
  expect((await state()).camera).toEqual(original.camera);
  await selector.selectOption(options[0]);
  expect((await state()).palette).toBe(original.palette);
  await host.getByRole('radio',{name:'Medial',exact:true}).check();
  await host.screenshot({path:root+'/report-medial.png'});
  expect(errors).toEqual([]);
  fs.writeFileSync(root+'/report-evidence.json',JSON.stringify({original,final:await state(),errors,customPaletteReset:true,mapSwitchCameraPreserved:true},null,2));
  await host.evaluate(n=>n.__nmSurfaceHandle.dispose());
  await page.close();
  console.log('Report palette reset, map switching, camera, opacity, and medial view passed.');
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1});
