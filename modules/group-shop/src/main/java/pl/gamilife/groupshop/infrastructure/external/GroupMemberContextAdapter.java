package pl.gamilife.groupshop.infrastructure.external;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.groupshop.domain.model.projection.GroupShopMember;
import pl.gamilife.groupshop.domain.port.context.GroupMemberContext;

import java.util.UUID;

@Component
@AllArgsConstructor
public class GroupMemberContextAdapter implements GroupMemberContext {
    GroupApi groupApi;

    @Override
    public GroupShopMember findMemberById(UUID memberId) {
        return new GroupShopMember(groupApi.findGroupMemberById(memberId).groupMemberId());
    }
}
