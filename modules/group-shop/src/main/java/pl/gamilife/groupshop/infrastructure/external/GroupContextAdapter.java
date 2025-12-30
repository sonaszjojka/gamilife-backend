package pl.gamilife.groupshop.infrastructure.external;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.groupshop.domain.model.projection.GroupShopMember;
import pl.gamilife.groupshop.domain.port.context.GroupContext;

import java.util.Optional;
import java.util.UUID;

@Component
@AllArgsConstructor
public class GroupContextAdapter implements GroupContext {

    private final GroupApi groupApi;

    @Override
    public GroupShopMember findMemberById(UUID groupId, UUID memberId) {
        var result = groupApi.findGroupMemberById(groupId, memberId);

        return new GroupShopMember(result.groupMemberId(), result.isAdmin());
    }

    @Override
    public Optional<GroupShopMember> findMemberByUserId(UUID userId, UUID groupId) {
        var result = groupApi.findGroupMemberByUserId(userId, groupId);

        return result.flatMap(gm -> Optional.of(new GroupShopMember(gm.groupMemberId(), gm.isAdmin())));
    }

    @Override
    public void payForItem(UUID memberId, Integer price) {
        groupApi.payForItem(memberId, price);
    }
}
