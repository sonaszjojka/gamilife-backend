package pl.gamilife.task.domain.port.context;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

public interface UserContext {
    LocalDate getCurrentUserDate(UUID userId);

    LocalDateTime getCurrentUserDateTime(UUID userId);
}
