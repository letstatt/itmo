import java.util.Map;
import java.util.Set;

public interface ConsistentHash<K> {
    Shard getShardByKey(K key);

    Map<Shard, Set<HashRange>> addShard(Shard newShard, Set<Integer> vnodeHashes);

    Map<Shard, Set<HashRange>> removeShard(Shard shard);
}
