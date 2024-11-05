export const updateWithData = <K extends string | number | symbol, V, T>(
  record: Record<K, V>,
  keysAndData: [K, T][],
  updateFn: (value: V, data: T) => V
): Record<K, V> => {
  const entries = keysAndData.reduce<[K, [V, T]][]>((acc, [key, data]) => {
    const value = record[key];
    const entry: [K, [V, T]] = [key, [value, data]];
    return value !== undefined ? [...acc, entry] : acc;
  }, []);

  if (entries.length !== keysAndData.length) {
    return record;
  }

  return entries.reduce(
    (acc, [key, [value, data]]) => ({ ...acc, [key]: updateFn(value, data) }),
    record
  );
};

export const update = <K extends string | number | symbol, V>(
  record: Record<K, V>,
  keys: K[],
  updateFn: (value: V) => V
): Record<K, V> => {
  const keysAndData: [K, undefined][] = keys.map((key) => [key, undefined]);
  return updateWithData(record, keysAndData, updateFn);
};
