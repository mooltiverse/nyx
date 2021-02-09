/*
 * Copyright 2020 Mooltiverse
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.mooltiverse.oss.nyx.gradle;

//import static org.junit.jupiter.api.Assertions.*;
//import static org.junit.jupiter.api.Assumptions.*;

//import java.io.File;
//import java.io.IOException;

//import org.gradle.api.Project;

//import org.gradle.testkit.runner.BuildResult;
//import org.gradle.testkit.runner.GradleRunner;

//import org.gradle.testfixtures.ProjectBuilder;

//import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
//import org.junit.jupiter.api.Nested;
//import org.junit.jupiter.api.Test;
//import org.junit.jupiter.api.TestInstance;

//import org.junit.jupiter.api.io.TempDir;

/**
 * Tests the Gradle plugin.<br>
 * 
 * See <a href="https://docs.gradle.org/current/userguide/test_kit.html"Testing Build Logic with TestKit</a> for more.
 */
@DisplayName("NyxGradlePlugin")
public class NyxGradlePluginTests {
    
/*
    @Nested
    @DisplayName("NyxGradlePlugin.apply")
    @TestInstance(TestInstance.Lifecycle.PER_CLASS)
    class GetBuildAttributeTests {
*/
        /**
         * The temporary project directory used for the tests
         */
/*
        @TempDir
        public File tempProjectDir;
    
        @BeforeAll
        public void setup()
            throws IOException {
            //settingsFile = tempProjectDir.newFile("settings.gradle");
            //buildFile = tempProjectDir.newFile("build.gradle");
        }

        @Test
        public void applyTest(){
            Project project = ProjectBuilder.builder().build();
            project.getPluginManager().apply("com.mooltiverse.oss.nyx");
        
            assertTrue(project.getPluginManager().hasPlugin("com.mooltiverse.oss.nyx"));

            //assertTrue(HelloNyxTask.class.isInstance(project.getTasks().getByName("helloNyx")));
        

            // withGradleVersion​(String versionNumber) allows to test against a specific Gradle version. We should test against version 2.8 and later. See https://docs.gradle.org/current/userguide/test_kit.html
            // GradleRunner.withDebug(boolean) enables debug output
            //BuildResult result = GradleRunner.create().withPluginClasspath().withProjectDir(tempProjectDir).withArguments("helloNyx").build();


            //assertNotNull(project.getTasks().getByName("helloNyx"));
        }
    }
    */
}