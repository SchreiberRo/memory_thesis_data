run("Duplicate...", " ");
setAutoThreshold("Default");
//run("Threshold...");
//setThreshold(0, 215);
setOption("BlackBackground", true);
run("Convert to Mask");
run("Skeletonize");
run("Analyze Skeleton (2D/3D)", "prune=none calculate show display");

