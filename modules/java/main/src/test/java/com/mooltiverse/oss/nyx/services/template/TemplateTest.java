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
package com.mooltiverse.oss.nyx.services.template;

import static org.junit.jupiter.api.Assertions.*;

import java.io.File;
import java.io.FileReader;
import java.io.StringWriter;
import java.net.URI;
import java.nio.file.Files;
import java.util.Map;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.state.State;
import com.mooltiverse.oss.nyx.services.Service;

@DisplayName("Template")
public class TemplateTest {
    @Nested
    @DisplayName("Instance")
    class InstanceTest {
        @Test
        public void exceptionWithNullOptions()
            throws Exception {
            assertThrows(NullPointerException.class, () -> Template.instance(null));
        }

        @Test
        public void instanceWithEmptyOptions()
            throws Exception {
            Template service = Template.instance(Map.<String,String>of());
            assertNotNull(service);
        }
    }

    @Nested
    @DisplayName("Supports")
    class SupportsTest {
        @ParameterizedTest(name = "Template.instance().supports(''{0}'') == true")
        @EnumSource(Service.Feature.class)
        public void supportAnyFeature(Service.Feature feature)
            throws Exception {
            if (Service.Feature.ASSET.equals(feature))
                assertTrue(Template.instance(Map.<String,String>of()).supports(feature));
            else assertFalse(Template.instance(Map.<String,String>of()).supports(feature));
        }
    }

    @Nested
    @DisplayName("Asset Service")
    class AssetServiceTest {
        @Test
        @DisplayName("Template.instance().buildAsset()")
        public void buildAsset()
            throws Exception {
            File destinationDir = Files.createTempDirectory("nyx-test-template-service-test-").toFile();
            File destinationFile = new File(destinationDir, "test-asset.md");

            assertFalse(destinationFile.exists());

            // pass the simplest template that just renders the version number from the state
            Template service = Template.instance(Map.<String,String>of(Template.TEMPLATE_OPTION_NAME, "{{version}}"));
            State state = new State(new Configuration());
            state.setVersion("1.5.7");
            URI target = service.buildAsset(destinationFile.getAbsolutePath(), state, null); // the Repository parameter is actually not used

            assertTrue(destinationFile.exists());
            assertEquals(destinationFile.toURI(), target);

            FileReader reader = new FileReader(destinationFile);
            StringWriter writer = new StringWriter();
            reader.transferTo(writer);
            reader.close();
            assertEquals("1.5.7", writer.toString());
        }
    }
}
