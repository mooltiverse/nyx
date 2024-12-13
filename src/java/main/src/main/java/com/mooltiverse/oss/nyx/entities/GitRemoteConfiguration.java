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
package com.mooltiverse.oss.nyx.entities;

/**
 * This object models the fields used to configure the remote Git repository.
 */
public class GitRemoteConfiguration {
    /**
     * The authentication method.
     */
    private AuthenticationMethod authenticationMethod = null;

    /**
     * The remote user name.
     */
    private String user = null;

    /**
     * The remote password.
     */
    private String password = null;

    /**
     * The private key.
     */
    private String privateKey = null;

    /**
     * The passphrase for the private key.
     */
    private String passphrase = null;

    /**
     * Default constructor.
     */
    public GitRemoteConfiguration() {
        super();
    }

    /**
     * Standard constructor.
     * 
     * @param authenticationMethod the authentication method.
     * @param user the remote user name.
     * @param password the remote password.
     * @param privateKey the private key.
     * @param passphrase the passphrase for the private key.
     */
    public GitRemoteConfiguration(AuthenticationMethod authenticationMethod, String user, String password, String privateKey, String passphrase) {
        super();
        this.authenticationMethod = authenticationMethod;
        this.user = user;
        this.password = password;
        this.privateKey = privateKey;
        this.passphrase = passphrase;
    }

    /**
     * Returns the authentication method.
     * 
     * @return the authentication method.
     */
    public AuthenticationMethod getAuthenticationMethod() {
        return authenticationMethod;
    }

    /**
     * Sets the authentication method.
     * 
     * @param authenticationMethod the authentication method.
     */
    public void setAuthenticationMethod(AuthenticationMethod authenticationMethod) {
        this.authenticationMethod = authenticationMethod;
    }

    /**
     * Returns the remote user name.
     * 
     * @return the remote user name.
     */
    public String getUser() {
        return user;
    }

    /**
     * Sets the remote user name.
     * 
     * @param user the remote user name.
     */
    public void setUser(String user) {
        this.user = user;
    }

    /**
     * Returns the remote password.
     * 
     * @return the remote password.
     */
    public String getPassword() {
        return password;
    }

    /**
     * Sets the remote password.
     * 
     * @param password the remote password.
     */
    public void setPassword(String password) {
        this.password = password;
    }

    /**
     * Returns the private key.
     * 
     * @return the private key.
     */
    public String getPrivateKey() {
        return privateKey;
    }

    /**
     * Sets the private key.
     * 
     * @param privateKey the private key.
     */
    public void setPrivateKey(String privateKey) {
        this.privateKey = privateKey;
    }

    /**
     * Returns the passphrase for the private key.
     * 
     * @return the passphrase for the private key.
     */
    public String getPassphrase() {
        return passphrase;
    }

    /**
     * Sets the passphrase for the private key.
     * 
     * @param passphrase the passphrase for the private key.
     */
    public void setPassphrase(String passphrase) {
        this.passphrase = passphrase;
    }
}
