import React from 'react';
import styles from './LoadingSpinner.module.css';

export const LoadingSpinner = () => {
    return (
        <div className={styles.loadingContainer}>
            <div className={styles.spinner}></div>
            <p>Verifying credentials...</p>
        </div>
    );
};