import React from 'react';
import { AuthWrapper } from '../auth/AuthWrapper';

export default function Root({ children }) {
    return <AuthWrapper>{children}</AuthWrapper>;
}