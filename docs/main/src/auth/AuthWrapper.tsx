import React, { useState, useEffect } from 'react';
import { LoginGoogle } from './components/Login/Login';
import { ColorModeProvider } from '@docusaurus/theme-common/internal';
import { auth } from './auth';
import { LoadingSpinner } from './components/LoadingSpinner/LoadingSpinner';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';

export const AuthWrapper = ({ children }: { children: React.ReactNode }) => {
    const [authToken, setAuthToken] = useState<string | null>(auth.getToken());
    const [isVerifying, setIsVerifying] = useState(true);
    const { siteConfig: { customFields } } = useDocusaurusContext();
    const googleClientId = customFields.googleClientId as string;
    const authEnabled = !!googleClientId;

    useEffect(() => {
        const verifyCurrentToken = async () => {
            if (authToken && authEnabled) {
                const isValid = await auth.verifyToken(authToken);
                if (!isValid) {
                    auth.logout();
                }
            }
            setIsVerifying(false);
        };

        verifyCurrentToken();
    }, [authEnabled]);

    const handleLogin = (token: string | null) => {
        if (token) {
            auth.setToken(token);
        }
        setAuthToken(token);
    };


    if (!authEnabled) {
        return <>{children}</>;
    }

    if (isVerifying) {
        return <LoadingSpinner />;
    }

    return (
        <>
            {!authToken ? (
                <ColorModeProvider>
                    <LoginGoogle login={handleLogin} />
                </ColorModeProvider>
            ) : (
                children
            )}
        </>
    );
};