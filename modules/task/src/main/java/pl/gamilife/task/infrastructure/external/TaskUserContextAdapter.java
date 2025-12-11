package pl.gamilife.task.infrastructure.external;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import pl.gamilife.task.domain.port.context.UserContext;

import java.time.ZoneId;
import java.util.UUID;

@Component
@AllArgsConstructor
public class TaskUserContextAdapter implements UserContext {
    @Override
    public ZoneId getUserTimeZone(UUID userId) {
        // TODO: Implement API call to get user timezone
        return null;
    }
}
