import clsx from 'clsx';
import Heading from '@theme/Heading';
import styles from './styles.module.css';

export type FeatureItem = {
  title: string;
  image: {
    src: string;
    width: number;
    height: number;
  };
  description: JSX.Element;
};

function Feature({title, image, description}: FeatureItem) {
  return (
    <div className={clsx('col')}>
      <div className="text--center">
        <img
          className={styles.featureImage}
          alt={title}
          width={Math.floor(image.width)}
          height={Math.floor(image.height)}
          src={image.src}
          loading="lazy"
        />
      </div>
      <div className="text--center padding-horiz--md">
        <Heading as="h3">{title}</Heading>
        <p>{description}</p>
      </div>
    </div>
  );
}

export default function HomepageFeatures({ features }): JSX.Element {
  const firstRow = features.slice(0, 3);
  const secondRow = features.slice(3, 6);
  const thirdRow = features.slice(6);

  return (
    <section className={styles.features}>
    <div className="container">
      <div className="row">
      {firstRow.map((props, idx) => (
        <Feature key={idx} {...props} />
      ))}
      </div>
      <div className="row">
      {secondRow.map((props, idx) => (
        <Feature key={idx} {...props} />
      ))}
      </div>
      <div className="row">
      {thirdRow.map((props, idx) => (
        <Feature key={idx} {...props} />
      ))}
      </div>
    </div>
    </section>
  );
}