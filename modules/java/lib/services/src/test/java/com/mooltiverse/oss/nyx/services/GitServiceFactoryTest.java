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
package com.mooltiverse.oss.nyx.services;

import static org.junit.jupiter.api.Assertions.*;

import java.net.URI;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.NullSource;

@DisplayName("GitServiceFactory")
public class GitServiceFactoryTest {
    @Nested
    @DisplayName("GitServiceFactory.instance(GitProvider)")
    class InstanceTests {
        @ParameterizedTest(name = "GitServiceFactory.instance(''{0}'').getClass() == ''{0}''.getServiceClass()")
        @EnumSource(GitProvider.class)
        void instance(GitProvider provider)
            throws Exception {
            assertEquals(provider.getServiceClass(), GitServiceFactory.instance(provider).getClass());
        }

        @ParameterizedTest(name = "GitServiceFactory.instance(null).getClass() throws NullPointerException")
        @NullSource()
        void exceptionUsingInstanceWithNullProvider(GitProvider provider)
            throws Exception {
            assertThrows(NullPointerException.class, () -> GitServiceFactory.instance(provider));
        }

        @ParameterizedTest(name = "GitServiceFactory.instance(''{0}'', (String)null).getClass() throws NullPointerException")
        @EnumSource(GitProvider.class)
        void exceptionUsingInstanceWithNullApiURLAsString(GitProvider provider)
            throws Exception {
            assertThrows(NullPointerException.class, () -> GitServiceFactory.instance(provider, (String)null));
        }

        @ParameterizedTest(name = "GitServiceFactory.instance(''{0}'', (URI)null).getClass() throws NullPointerException")
        @EnumSource(GitProvider.class)
        void exceptionUsingInstanceWithNullApiURLAsURI(GitProvider provider)
            throws Exception {
            assertThrows(NullPointerException.class, () -> GitServiceFactory.instance(provider, (URI)null));
        }

        @ParameterizedTest(name = "GitServiceFactory.instance(''{0}'', (String)'''').getClass() throws IllegalArgumentException")
        @EnumSource(GitProvider.class)
        void exceptionUsingInstanceWithEmptyApiURLAsString(GitProvider provider)
            throws Exception {
            assertThrows(IllegalArgumentException.class, () -> GitServiceFactory.instance(provider, ""));
            assertThrows(IllegalArgumentException.class, () -> GitServiceFactory.instance(provider, "  "));
        }

        @ParameterizedTest(name = "GitServiceFactory.instance(''{0}'', (URI)'''').getClass() throws IllegalArgumentException")
        @EnumSource(GitProvider.class)
        void exceptionUsingInstanceWithEmptyApiURLAsURI(GitProvider provider)
            throws Exception {
            assertThrows(IllegalArgumentException.class, () -> GitServiceFactory.instance(provider, URI.create("")));
            //assertThrows(IllegalArgumentException.class, () -> GitServiceFactory.instance(provider, URI.create("  "))); // In this case it's the URI constructor raising an exception
        }

        @ParameterizedTest(name = "GitServiceFactory.instance(''{0}'', (String)''any string'').getClass() throws IllegalArgumentException")
        @EnumSource(GitProvider.class)
        void exceptionUsingInstanceWithMalformedApiURLAsString(GitProvider provider)
            throws Exception {
            assertThrows(IllegalArgumentException.class, () -> GitServiceFactory.instance(provider, "this string can't be an URI"));
        }
    }
}