const AUTH_KEY = 'auth_token';

const isBrowser = () => typeof window !== 'undefined';

export const auth = {
    setToken: (token: string) => {
        if (isBrowser()) {
            localStorage.setItem(AUTH_KEY, token);
        }
    },
    getToken: () => {
        if (isBrowser()) {
            return localStorage.getItem(AUTH_KEY);
        }
        return null;
    },
    clearToken: () => {
        if (isBrowser()) {
            localStorage.removeItem(AUTH_KEY);
        }
    },
    isAuthenticated: () => {
        if (isBrowser()) {
            return !!localStorage.getItem(AUTH_KEY);
        }
        return false;
    },
    verifyToken: async (token: string) => {
        try {
            const response = await fetch('https://oauth2.googleapis.com/tokeninfo?id_token=' + token);
            return response.status === 200;
        } catch (error) {
            console.error('Token verification failed:', error);
            return false;
        }
    },
    logout: () => {
        if (isBrowser()) {
            localStorage.removeItem(AUTH_KEY);
            window.location.reload();
        }
    }
};



