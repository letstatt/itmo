import java.util.*;

public class ConsistentHashImpl<K> implements ConsistentHash<K> {
    private final TreeMap<Integer, Shard> vnodes = new TreeMap<>();

    private Shard getShardByHash(int hash) {
        if (vnodes.isEmpty()) {
            throw new NoSuchElementException();
        }
        var entry = vnodes.ceilingEntry(hash);
        if (entry != null) {
            return entry.getValue();
        } else {
            return vnodes.firstEntry().getValue();
        }
    }

    @Override
    public Shard getShardByKey(K key) {
        return getShardByHash(key.hashCode());
    }

    @Override
    public Map<Shard, Set<HashRange>> addShard(Shard newShard, Set<Integer> vnodeHashes) {
        if (vnodes.isEmpty()) {
            vnodeHashes.forEach(vnode -> vnodes.put(vnode, newShard));
            return Map.of();
        } else {
            Map<Shard, Set<HashRange>> delegated = new HashMap<>();
            Map<Integer, Integer> segments = new HashMap<>();
            for (var vnodeHash: vnodeHashes) {
                if (vnodes.containsKey(vnodeHash)) {
                    throw new IllegalArgumentException("Hash collision with vnodeHash=" + vnodeHash);
                }
                var key = vnodes.lowerKey(vnodeHash);
                if (key == null) {
                    key = vnodes.lastKey();
                }
                segments.compute(key, (k, v) -> {
                    if (v == null) {
                        return vnodeHash;
                    } else if (k < v && k < vnodeHash || k > v && k > vnodeHash) {
                        return Math.max(v, vnodeHash);
                    } else {
                        // keep it cycled
                        return Math.min(v, vnodeHash);
                    }
                });
            }
            for (var entry: segments.entrySet()) {
                var ceilingShard = getShardByHash(entry.getKey() + 1);
                var setOfHashRanges = delegated.computeIfAbsent(ceilingShard, sh -> new HashSet<>());
                setOfHashRanges.add(new HashRange(entry.getKey() + 1, entry.getValue()));
            }
            vnodeHashes.forEach(vnode -> vnodes.put(vnode, newShard));
            return delegated;
        }
    }

    @Override
    public Map<Shard, Set<HashRange>> removeShard(Shard shard) {
        if (vnodes.isEmpty()) {
            throw new NoSuchElementException();
        } else {
            ArrayList<HashRange> segments = new ArrayList<>();
            for (var entry: vnodes.entrySet()) {
                if (!entry.getValue().equals(shard)) {
                    continue;
                }
                var floorKey = vnodes.lowerKey(entry.getKey());
                if (floorKey == null) {
                    floorKey = vnodes.lastKey();
                }
                if (segments.isEmpty()) {
                    segments.add(new HashRange(floorKey + 1, entry.getKey()));
                } else {
                    var lastSegment = segments.get(segments.size() - 1);
                    if (lastSegment.getRightBorder() == floorKey) {
                        lastSegment.setRightBorder(entry.getKey());
                    } else {
                        segments.add(new HashRange(floorKey + 1, entry.getKey()));
                    }
                }
            }
            if (segments.isEmpty()) {
                throw new NoSuchElementException();
            }
            // keep it cycled
            if (segments.get(segments.size() - 1).getRightBorder() + 1 == segments.get(0).getLeftBorder()) {
                segments.get(0).setLeftBorder(segments.get(segments.size() - 1).getLeftBorder());
                segments.remove(segments.size() - 1);
            }
            Map<Shard, Set<HashRange>> delegated = new HashMap<>();
            for (var segment: segments) {
                var ceilingShard = getShardByHash(segment.getRightBorder() + 1);
                var setOfHashRanges = delegated.computeIfAbsent(ceilingShard, sh -> new HashSet<>());
                setOfHashRanges.add(segment);
            }
            vnodes.entrySet().removeIf(entry -> entry.getValue().equals(shard));
            return delegated;
        }
    }
}
