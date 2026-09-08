# Run from the neuromosaic root after installing the matching R dependencies.
# RGL_USE_NULL=TRUE Rscript tools/render-surface-contrast.R
# Optional NM_SURFACE_BASELINE_RUNTIME pins the comparison to an older bundle.
library(neurosurf)
library(neuroatlas)
pkgload::load_all('.', quiet=TRUE)
dir.create(file.path('e2e', '.artifacts'), recursive=TRUE, showWarnings=FALSE)
out <- normalizePath(file.path('e2e', '.artifacts'), mustWork=TRUE)
out <- file.path(out, 'surface-contrast')
dir.create(out, recursive=TRUE, showWarnings=FALSE)
data('fsaverage', package='neuroatlas')
repair <- function(g, hemi) SurfaceGeometry(coords(g), faces(g)-1L, hemi=hemi)
geometry <- list(left=repair(fsaverage$lh_inflated, 'left'),
                 right=repair(fsaverage$rh_inflated, 'right'))
labeled <- function(g) methods::new('LabeledNeuroSurface', geometry=g,
  indices=as.integer(seq_len(nrow(coords(g)))), data=rep(1, nrow(coords(g))),
  labels='cortex', cols='#888888')
atlas <- structure(list(lh_atlas=labeled(geometry$left), rh_atlas=labeled(geometry$right),
  ids=1, labels='cortex', surf_type='inflated', surface_space='fsaverage6'), class='surfatlas')
anatomy <- lapply(c('left','right'), function(h) surface_anatomy(atlas,h))
names(anatomy) <- c('left','right')
stopifnot(all(vapply(anatomy, function(a) a$provenance$topology_verified, logical(1))))
# Analytic synthetic signal on the original folded coordinates. This is a
# display fixture, not subject data and not inferential evidence.
signal <- function(g) {
  xyz <- coords(g)
  4.5*sin(xyz[,2]/18)*cos(xyz[,3]/23) + 0.5*sin(xyz[,1]/9)
}
values <- list(left=signal(fsaverage$lh_white), right=signal(fsaverage$rh_white))
saveRDS(list(geometry=geometry, atlas=atlas, anatomy=anatomy, values=values),
        file.path(out,'fixture.rds'))
for (variant in c('before','continuous','binary')) {
  config <- montage_surface(preset=if(variant=='before') 'paper-light' else 'freesurfer',
    anatomy_style=if(variant=='before') 'none' else variant)
  a <- neuromosaic:::.montage_surface_anatomy(atlas,geometry,config,list())
  scene <- surface_scene(left=geometry$left, right=geometry$right,
    curvature=a$curvature, preset=config$preset, id=paste0('contrast-',variant),
    fallback='Surface unavailable', alt_text='Synthetic cortical contrast comparison',
    layers=surface_layer('statistic',values,
      colormap=if(variant=='before') 'RdBu' else 'surface-heat',
      limits=c(-5,5), threshold=c(-2,2), opacity=if(variant=='before') .85 else 1,
      legend=list(title='Synthetic signed statistic', units='z')))
  htmlwidgets::saveWidget(surfwidget(scene, width='100%', height=650),
    file.path(out,paste0(variant,'.html')), selfcontained=FALSE,
    libdir=paste0(variant,'-files'))
}
for (view in c('lateral','medial')) {
  panels <- lapply(c('left','right'), function(h) {
    render_surface_rgba(geometry[[h]],values[[h]],
      anatomy_metric=anatomy[[h]]$metric, anatomy_style='binary',
      anatomy_range=c(.25,.75), palette=surface_heat_colors(), limits=c(-5,5),
      threshold=2, overlay_alpha=1, width=800, height=600, camera=view,
      background='black', outer_contour=FALSE)
  })
  img <- array(0,c(600,1600,4))
  img[,1:800,] <- as.numeric(panels[[1]]$rgba)/255
  img[,801:1600,] <- as.numeric(panels[[2]]$rgba)/255
  png::writePNG(img,file.path(out,paste0('static-',view,'.png')))
}
baseline <- Sys.getenv('NM_SURFACE_BASELINE_RUNTIME')
if (nzchar(baseline)) {
  runtime <- neuromosaic:::.montage_surface_runtime_info()
  target <- file.path(out, 'before-files', paste0('surfview-', runtime$version),
                      runtime$script)
  stopifnot(file.copy(baseline, target, overwrite=TRUE))
}
cat(out,'\n')

x <- readRDS(file.path(out,'fixture.rds'))
names(x$values) <- c('lh','rh')
p <- plot_brain(x$atlas, vals=rep(0,1), interactive=FALSE,
  static_backend='cpu', overlay=x$values, overlay_threshold=2,
  overlay_lim=c(-5,5), overlay_palette=neurosurf::surface_heat_colors(),
  overlay_alpha=1, overlay_alpha_ramp=0,
  anatomy_style='binary', anatomy_range=c(.25,.75),
  views=c('lateral','medial'), hemis=c('left','right'),
  overlay_title='Synthetic signed statistic (z)', colorbar_source='overlay',
  colorbar=TRUE, colorbar_position='bottom', title='Cortical contrast - static',
  render_width=800, render_height=600, bg='white')
ggplot2::ggsave(file.path(out,'static-montage.png'),p,width=12,height=9,dpi=150)
stopifnot(length(attr(p,'plot_brain_colorbar')$palette)==256)
cat('Static montage and 256-color legend rendered.\n')

x <- readRDS(file.path(out,'fixture.rds'))
m <- data.frame(map_id=c('signal','comparison'),analysis_id='contrast',
 role=c('primary','auxiliary'), quantity='test_statistic', distribution='z',
 units='z',signed=TRUE,threshold=2,tail='two_sided',
 label=c('Synthetic signed statistic','Comparison statistic'),
 selector_label=c('Signal','Comparison'))
m$parcel_values <- I(list(c(`1`=5),c(`1`=5)))
panels <- list(signal=list(surface_image=file.path(out,'static-montage.png')),
 comparison=list(surface_image=file.path(out,'static-montage.png')))
render_montage_report(m, output_file=file.path(out,'report.html'),
 title='Cortical contrast verification',surfatlas=x$atlas,panels=panels,
 render_volume=FALSE,render_surface=FALSE,materialize_recipes=FALSE,
 check_files=FALSE,surface=montage_surface(preset='freesurfer',assets='embed',
 values=list(signal=x$values,comparison=lapply(x$values,`*`,.8))))
