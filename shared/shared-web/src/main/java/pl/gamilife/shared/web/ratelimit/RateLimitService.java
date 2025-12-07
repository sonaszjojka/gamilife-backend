package pl.gamilife.shared.web.ratelimit;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import io.github.bucket4j.Bandwidth;
import io.github.bucket4j.Bucket;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.time.Duration;
import java.util.concurrent.TimeUnit;

@Service
public class RateLimitService {

    private final Cache<String, Bucket> bucketCache = Caffeine.newBuilder()
            .expireAfterAccess(1, TimeUnit.HOURS)
            .build();
    @Value("${app.rate-limit.requests-limit}")
    private int requestsLimit;
    @Value("${app.rate-limit.duration-seconds}")
    private int durationSeconds;

    public Bucket getBucket(String key) {
        return bucketCache.get(key, this::createNewBucket);
    }

    public Bucket createNewBucket(String key) {
        Bandwidth bandwidth = Bandwidth.builder()
                .capacity(requestsLimit)
                .refillGreedy(requestsLimit, Duration.ofSeconds(durationSeconds))
                .build();

        return Bucket.builder()
                .addLimit(bandwidth)
                .build();
    }
}
