package pl.gamilife.task.infrastructure.external;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.task.domain.port.context.UserContext;

import java.time.ZoneId;
import java.util.UUID;

@Component
@AllArgsConstructor
public class TaskUserContextAdapter implements UserContext {

    private final UserApi userApi;

    @Override
    public ZoneId getCurrentUserTimezone(UUID userId) {
        return userApi.getUserZoneId(userId);
    }
}
