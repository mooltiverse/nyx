import clsx from 'clsx';
import Heading from '@theme/Heading';
import Link from '@docusaurus/Link';
import { Icon } from "@iconify/react";
import styles from './styles.module.css';

export interface LinkProps {
    title: string;
    url: string;
    image: {
      src: string;
      width: number;
      height: number;
      color: string;
    };
}

export type ResourceItem = {
    name: string;
    environment: string;
    url: string;
    description: JSX.Element;
    image: {
      src: string;
      width: number;
      height: number;
      color: string;
    };
    links?: LinkProps[];
};

function Resource({name, environment, url, description, image, links}: ResourceItem): JSX.Element {
    return (
      <div className={clsx('card', styles.resource)}>
        <div className="card__header">
          <div className="avatar">
            <Link className={clsx(styles.resourceMeta)} to={url}>
              <Icon 
                icon={image.src}
                alt={name}
                width={Math.floor(image.width)}
                height={Math.floor(image.height)}
                color={image.color}
                className={styles.featureIcon}
              />
            </Link>
            <div className={clsx('avatar__intro', styles.resourceMeta)}>
              <Link className={clsx(styles.resourceMeta)} to={url}>
                <strong className="avatar__name">{name}</strong>
              </Link>
              <span>{environment}</span>
            </div>
          </div>
        </div>
  
        <div className={clsx('card__body', styles.resource)}>{description}</div>
  
        {links && links.length > 0 && (
          <div className={styles.linksWrapper}>
            <div className={styles.links}>
              {links.map((link, index) => (
                <Link className={clsx(styles.link)} to={link.url}>
                  <Icon 
                    icon={link.image && link.image.src || image.src}
                    alt={name}
                    width={Math.floor(link.image && link.image.width || image.width)}
                    height={Math.floor(link.image && link.image.height || image.height)}
                    color={link.image && link.image.color || image.color}
                    className={styles.linkIcon}
                  />
                {link.title}
                </Link>
              ))}
            </div>
          </div>
        )}
      </div>
    );
  }

export default function FeaturedResources({ resources }) {
    const resourceColumns: ResourceItem[][] = [[], [], []];
    resources.forEach((resource, i) =>
        resourceColumns[i % 3]!.push(resource),
    );

    return (
        <div className={clsx(styles.section, styles.sectionAlt)}>
        <div className="container">
            <Heading as="h2" className={clsx('margin-bottom--lg', 'text--center')}>
            Featured Resources
            </Heading>
            <div className={clsx('row', styles.resourcesSection)}>
            {resourceColumns.map((resourceItems, i) => (
                <div className="col col--4" key={i}>
                {resourceItems.map((resource) => (
                    <Resource {...resource} />
                ))}
                </div>
            ))}
            </div>
        </div>
        </div>
    );
}