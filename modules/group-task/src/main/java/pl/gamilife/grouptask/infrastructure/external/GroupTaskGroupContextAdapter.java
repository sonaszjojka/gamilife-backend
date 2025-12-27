package pl.gamilife.grouptask.infrastructure.external;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.grouptask.domain.context.GroupContext;

import java.time.ZoneId;
import java.util.UUID;

@Component
@AllArgsConstructor
public class GroupTaskGroupContextAdapter implements GroupContext {

    private final GroupApi groupApi;

    @Override
    public ZoneId getCurrentGroupTimezone(UUID groupId) {
        return groupApi.getGroupTimezone(groupId);
    }
}
