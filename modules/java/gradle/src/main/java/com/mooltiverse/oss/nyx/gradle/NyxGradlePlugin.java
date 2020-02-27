package com.mooltiverse.oss.nyx.gradle;

import org.gradle.api.Plugin;
import org.gradle.api.Project;

public class NyxGradlePlugin implements Plugin<Project> {
    public void apply(Project project) {
      project.getExtensions().create("release", NyxGradlePluginExtension.class);
//        project.task("hello").doLast (null);
    }
}
