UPDATE registration SET swim = FALSE WHERE swim IS NULL;
ALTER TABLE registration ALTER COLUMN swim SET DEFAULT FALSE;
ALTER TABLE registration ALTER COLUMN swim SET NOT NULL;
