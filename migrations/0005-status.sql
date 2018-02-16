ALTER TABLE registration ADD COLUMN status TEXT;
UPDATE registration SET status = 'N' WHERE status IS NULL;
ALTER TABLE registration ALTER COLUMN swim SET NOT NULL;
