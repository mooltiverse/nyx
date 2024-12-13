import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import { GoogleOAuthProvider, useGoogleOneTapLogin, GoogleLogin } from '@react-oauth/google';
import React from 'react';
import { useColorMode } from '@docusaurus/theme-common';
import ColorModeToggle from '@theme/ColorModeToggle';
import styles from './Login.module.css';

export function LoginGoogle({ login }: { login: (value: string | null) => void }) {
    const [denied, setDenied] = React.useState(false);
    const {
        siteConfig: { customFields },
    } = useDocusaurusContext();

    const clientId = customFields.googleClientId as string;

    function handleSuccess(credentialResponse: any) {
        if (credentialResponse.credential) {
            // Store and pass the JWT token
            login(credentialResponse.credential);
        } else {
            login(null);
            setDenied(true);
        }
    }

    function LoginContent() {
        const { colorMode, setColorMode } = useColorMode();
        useGoogleOneTapLogin({
            onSuccess: handleSuccess,
            onError: () => setDenied(true),
        });

        return (
            <div className={styles.loginContainer}>
                <div className={styles.loginCard}>
                    <div className={styles.themeToggleContainer}>
                        <ColorModeToggle
                            className={styles.themeToggle}
                            value={colorMode}
                            onChange={setColorMode}
                        />
                    </div>
                    <h2>Authentication</h2>
                    <p>Please sign in to continue</p>
                    <div className={styles.googleButtonContainer}>
                        <GoogleLogin
                            onSuccess={handleSuccess}
                            onError={() => setDenied(true)}
                            useOneTap
                            theme={colorMode === 'dark' ? 'filled_black' : 'outline'}
                            shape="rectangular"
                            text="continue_with"
                            type="standard"
                            auto_select
                            itp_support
                        />
                    </div>
                    {denied && (
                        <div className={styles.errorMessage}>
                            Access denied.
                        </div>
                    )}
                </div>
            </div>
        );
    }

    return (
        <GoogleOAuthProvider clientId={clientId}>
            <LoginContent />
        </GoogleOAuthProvider>
    );
}