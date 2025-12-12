package pl.gamilife.task.domain.port.context;

import java.time.ZoneId;
import java.util.UUID;

public interface UserContext {
    ZoneId getCurrentUserTimezone(UUID userId);
}
