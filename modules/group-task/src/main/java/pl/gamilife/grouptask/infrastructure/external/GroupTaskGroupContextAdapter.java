package pl.gamilife.grouptask.infrastructure.external;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import pl.gamilife.grouptask.domain.context.GroupContext;

import java.time.LocalDateTime;
import java.util.UUID;

@Component
@AllArgsConstructor
public class GroupTaskGroupContextAdapter implements GroupContext {
    @Override
    public LocalDateTime getCurrentGroupDateTime(UUID groupId) {
        // TODO: implement
        throw new RuntimeException("Not yet implemented");
    }
}
