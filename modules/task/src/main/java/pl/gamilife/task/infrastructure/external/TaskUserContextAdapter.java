package pl.gamilife.task.infrastructure.external;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import pl.gamilife.task.domain.port.context.UserContext;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Component
@AllArgsConstructor
public class TaskUserContextAdapter implements UserContext {
    @Override
    public LocalDate getCurrentUserDate(UUID userId) {
        // TODO: Implement API call to get user timezone
        return null;
    }

    @Override
    public LocalDateTime getCurrentUserDateTime(UUID userId) {
        return null;
    }
}
