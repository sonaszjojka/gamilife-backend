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

    private final Cache<String, Bucket> standardCacheBucket = Caffeine.newBuilder()
            .expireAfterAccess(1, TimeUnit.HOURS)
            .build();

    private final Cache<String, Bucket> protectedCacheBucket = Caffeine.newBuilder()
            .expireAfterAccess(1, TimeUnit.HOURS)
            .build();

    @Value("${app.rate-limit.standard.requests-limit}")
    private int standardRequestsLimit;
    @Value("${app.rate-limit.standard.duration-seconds}")
    private int standardDurationSeconds;

    @Value("${app.rate-limit.protected.requests-limit}")
    private int protectedRequestsLimit;
    @Value("${app.rate-limit.protected.duration-seconds}")
    private int protectedDurationSeconds;

    public Bucket getBucket(String key, String uri) {
        boolean isProtected = uri.contains("/api/v1/auth") || uri.contains("/api/v1/oauth2");
        return isProtected
                ? protectedCacheBucket.get(key, this::createNewProtectedBucket)
                : standardCacheBucket.get(key, this::createNewStandardBucket);
    }

    private Bucket createNewStandardBucket(String key) {
        Bandwidth bandwidth = Bandwidth.builder()
                .capacity(standardRequestsLimit)
                .refillGreedy(standardRequestsLimit, Duration.ofSeconds(standardDurationSeconds))
                .build();

        return Bucket.builder()
                .addLimit(bandwidth)
                .build();
    }

    private Bucket createNewProtectedBucket(String key) {
        Bandwidth intervalLimit = Bandwidth.builder()
                .capacity(protectedRequestsLimit)
                .refillIntervally(protectedRequestsLimit, Duration.ofSeconds(protectedDurationSeconds))
                .build();

        // Limit "Twice per Second, renew every 2 seconds"
        Bandwidth instantBurstLimit = Bandwidth.builder()
                .capacity(2)
                .refillGreedy(2, Duration.ofSeconds(2))
                .build();

        return Bucket.builder()
                .addLimit(intervalLimit)
                .addLimit(instantBurstLimit)
                .build();
    }
}
