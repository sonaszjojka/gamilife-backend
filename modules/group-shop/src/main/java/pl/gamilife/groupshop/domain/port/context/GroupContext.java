package pl.gamilife.groupshop.domain.port.context;

import pl.gamilife.groupshop.domain.model.projection.GroupShopMember;

import java.util.Optional;
import java.util.UUID;

public interface GroupContext {
    GroupShopMember findMemberById(UUID groupId, UUID memberId);

    Optional<GroupShopMember> findMemberByUserId(UUID userId, UUID groupId);
}
