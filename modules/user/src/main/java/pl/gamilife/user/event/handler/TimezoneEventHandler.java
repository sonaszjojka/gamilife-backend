package pl.gamilife.user.event.handler;

import com.github.benmanes.caffeine.cache.Cache;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.event.EventListener;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import pl.gamilife.shared.kernel.event.TimezoneDetectedEvent;
import pl.gamilife.user.usecase.updateusertimezone.UpdateUserTimezoneCommand;
import pl.gamilife.user.usecase.updateusertimezone.UpdateUserTimezoneUseCase;

import java.util.UUID;

@Slf4j
@Component
@AllArgsConstructor
public class TimezoneEventHandler {

    private final UpdateUserTimezoneUseCase updateUserTimezoneUseCase;
    private final Cache<UUID, String> userTimezoneCache;

    @Async("eventExecutor")
    @EventListener
    public void onTimezoneDetected(TimezoneDetectedEvent event) {
        log.info("Timezone detected event caught: {}", event);
        String zoneFromCache = userTimezoneCache.getIfPresent(event.userId());
        log.info("Found {} in cache for user {}", zoneFromCache, event.userId());
        if (event.timezone().equals(zoneFromCache)) {
            return;
        }

        updateUserTimezoneUseCase.execute(new UpdateUserTimezoneCommand(event.userId(), event.timezone()));

        userTimezoneCache.put(event.userId(), event.timezone());
        log.info("Successful update of timezone {} -> {}", zoneFromCache, event.timezone());
    }

}
