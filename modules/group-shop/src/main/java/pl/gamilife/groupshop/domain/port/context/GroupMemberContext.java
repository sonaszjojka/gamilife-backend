package pl.gamilife.groupshop.domain.port.context;

import pl.gamilife.groupshop.domain.model.projection.GroupShopMember;

import java.util.Optional;
import java.util.UUID;

public interface GroupMemberContext {
    GroupShopMember findMemberById(UUID memberId);

    Optional<GroupShopMember> findMemberByUserId(UUID userId, UUID groupId);
}
