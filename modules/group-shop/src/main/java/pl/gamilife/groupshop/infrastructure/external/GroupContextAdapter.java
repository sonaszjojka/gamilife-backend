package pl.gamilife.groupshop.infrastructure.external;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.groupshop.domain.model.projection.GroupForShop;
import pl.gamilife.groupshop.domain.port.context.GroupContext;

import java.util.UUID;

@Component
@AllArgsConstructor
public class GroupContextAdapter implements GroupContext {

    GroupApi groupApi;

    @Override
    public GroupForShop findGroupById(UUID groupId) {
        return new GroupForShop(groupApi.findGroupById(groupId).groupId(), groupApi.findGroupById(groupId).adminId());
    }
}
