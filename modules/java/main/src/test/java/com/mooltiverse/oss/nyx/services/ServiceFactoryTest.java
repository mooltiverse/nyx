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

import java.util.Map;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.NullSource;

@DisplayName("ServiceFactory")
public class ServiceFactoryTest {
    @Nested
    @DisplayName("ServiceFactory.gitHostingServiceInstance()")
    class GitHostingServiceInstanceTests {
        @ParameterizedTest(name = "ServiceFactory.gitHostingServiceInstance(''{0}'', Map<String,Object>).getClass() == ''{0}''.getServiceClass()")
        @EnumSource(Provider.class)
        void gitHostingServiceInstance(Provider provider)
            throws Exception {
            if (provider.equals(Provider.GITHUB) || provider.equals(Provider.GITLAB))
                assertEquals(provider.getServiceClass(), ServiceFactory.gitHostingServiceInstance(provider, Map.<String,String>of()).getClass());
            else assertThrows(UnsupportedOperationException.class, () -> ServiceFactory.gitHostingServiceInstance(provider, Map.<String,String>of()));
        }

        @ParameterizedTest(name = "ServiceFactory.gitHostingServiceInstance(null, Map<String,Object>).getClass() throws NullPointerException")
        @NullSource()
        void exceptionUsingGitHostingServiceInstanceWithNullProvider(Provider provider)
            throws Exception {
            assertThrows(NullPointerException.class, () -> ServiceFactory.gitHostingServiceInstance(provider, Map.<String,String>of()));
        }
    }

    @Nested
    @DisplayName("ServiceFactory.gitLocalServiceInstance()")
    class GitLocalServiceInstanceTests {
        @ParameterizedTest(name = "ServiceFactory.gitLocalServiceInstance(''{0}'', Map<String,Object>).getClass() == ''{0}''.getServiceClass()")
        @EnumSource(Provider.class)
        void gitLocalServiceInstance(Provider provider)
            throws Exception {
            if (provider.equals(Provider.GIT))
                assertEquals(provider.getServiceClass(), ServiceFactory.gitLocalServiceInstance(provider, Map.<String,String>of()).getClass());
            else assertThrows(UnsupportedOperationException.class, () -> ServiceFactory.gitLocalServiceInstance(provider, Map.<String,String>of()));
        }

        @ParameterizedTest(name = "ServiceFactory.gitLocalServiceInstance(null, Map<String,Object>).getClass() throws NullPointerException")
        @NullSource()
        void exceptionUsingGitLocalServiceInstanceWithNullProvider(Provider provider)
            throws Exception {
            assertThrows(NullPointerException.class, () -> ServiceFactory.gitLocalServiceInstance(provider, Map.<String,String>of()));
        }
    }

    @Nested
    @DisplayName("ServiceFactory.gitRemoteServiceInstance()")
    class GitRemoteServiceInstanceTests {
        @ParameterizedTest(name = "ServiceFactory.gitRemoteServiceInstance(''{0}'', Map<String,Object>).getClass() == ''{0}''.getServiceClass()")
        @EnumSource(Provider.class)
        void gitRemoteServiceInstance(Provider provider)
            throws Exception {
            if (provider.equals(Provider.GIT) || provider.equals(Provider.GITHUB) || provider.equals(Provider.GITLAB))
                assertEquals(provider.getServiceClass(), ServiceFactory.gitRemoteServiceInstance(provider, Map.<String,String>of()).getClass());
            else assertThrows(UnsupportedOperationException.class, () -> ServiceFactory.gitRemoteServiceInstance(provider, Map.<String,String>of()));
        }

        @ParameterizedTest(name = "ServiceFactory.gitRemoteServiceInstance(null, Map<String,Object>).getClass() throws NullPointerException")
        @NullSource()
        void exceptionUsingGitRemoteServiceInstanceWithNullProvider(Provider provider)
            throws Exception {
            assertThrows(NullPointerException.class, () -> ServiceFactory.gitRemoteServiceInstance(provider, Map.<String,String>of()));
        }
    }

    @Nested
    @DisplayName("ServiceFactory.instance()")
    class InstanceTests {
        @ParameterizedTest(name = "ServiceFactory.instance(''{0}'', Map<String,Object>).getClass() == ''{0}''.getServiceClass()")
        @EnumSource(Provider.class)
        void instance(Provider provider)
            throws Exception {
            assertEquals(provider.getServiceClass(), ServiceFactory.instance(provider, Map.<String,String>of()).getClass());
        }

        @ParameterizedTest(name = "ServiceFactory.instance(null, Map<String,Object>).getClass() throws NullPointerException")
        @NullSource()
        void exceptionUsingInstanceWithNullProvider(Provider provider)
            throws Exception {
            assertThrows(NullPointerException.class, () -> ServiceFactory.instance(provider, Map.<String,String>of()));
        }
    }

    @Nested
    @DisplayName("ServiceFactory.releaseServiceInstance()")
    class ReleaseServiceInstanceTests {
        @ParameterizedTest(name = "ServiceFactory.releaseServiceInstance(''{0}'', Map<String,Object>).getClass() == ''{0}''.getServiceClass()")
        @EnumSource(Provider.class)
        void releaseServiceInstance(Provider provider)
            throws Exception {
            if (provider.equals(Provider.GITHUB) || provider.equals(Provider.GITLAB))
                assertEquals(provider.getServiceClass(), ServiceFactory.releaseServiceInstance(provider, Map.<String,String>of()).getClass());
            else assertThrows(UnsupportedOperationException.class, () -> ServiceFactory.releaseServiceInstance(provider, Map.<String,String>of()));
        }

        @ParameterizedTest(name = "ServiceFactory.releaseServiceInstance(null, Map<String,Object>).getClass() throws NullPointerException")
        @NullSource()
        void exceptionUsingReleaseServiceInstanceWithNullProvider(Provider provider)
            throws Exception {
            assertThrows(NullPointerException.class, () -> ServiceFactory.releaseServiceInstance(provider, Map.<String,String>of()));
        }
    }

    @Nested
    @DisplayName("ServiceFactory.userServiceInstance()")
    class UserServiceInstanceTests {
        @ParameterizedTest(name = "ServiceFactory.userServiceInstance(''{0}'', Map<String,Object>).getClass() == ''{0}''.getServiceClass()")
        @EnumSource(Provider.class)
        void userServiceInstance(Provider provider)
            throws Exception {
            if (provider.equals(Provider.GITHUB) || provider.equals(Provider.GITLAB))
                assertEquals(provider.getServiceClass(), ServiceFactory.userServiceInstance(provider, Map.<String,String>of()).getClass());
            else assertThrows(UnsupportedOperationException.class, () -> ServiceFactory.userServiceInstance(provider, Map.<String,String>of()));
        }

        @ParameterizedTest(name = "ServiceFactory.userServiceInstance(null, Map<String,Object>).getClass() throws NullPointerException")
        @NullSource()
        void exceptionUsingUserServiceInstanceWithNullProvider(Provider provider)
            throws Exception {
            assertThrows(NullPointerException.class, () -> ServiceFactory.userServiceInstance(provider, Map.<String,String>of()));
        }
    }
}