package pl.gamilife.groupshop.infrastructure.external;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.groupshop.domain.model.projection.GroupShopMember;
import pl.gamilife.groupshop.domain.port.context.GroupMemberContext;

import java.util.Optional;
import java.util.UUID;

@Component
@AllArgsConstructor
public class GroupMemberContextAdapter implements GroupMemberContext {
    GroupApi groupApi;

    @Override
    public GroupShopMember findMemberById(UUID memberId) {
        return new GroupShopMember(groupApi.findGroupMemberById(memberId).groupMemberId());
    }

    @Override
    public Optional<GroupShopMember> findMemberByUserId(UUID userId, UUID groupId) {
        var result = groupApi.findGroupMemberByUserId(userId, groupId);

        return result.flatMap(gm -> Optional.of(new GroupShopMember(gm.groupMemberId())));
    }
}
