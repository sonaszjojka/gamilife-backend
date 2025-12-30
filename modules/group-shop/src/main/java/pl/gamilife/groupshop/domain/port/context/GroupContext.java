package pl.gamilife.groupshop.domain.port.context;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.groupshop.domain.model.projection.GroupShopMember;

import java.util.Optional;
import java.util.UUID;

public interface GroupContext {
    GroupShopMember findMemberById(UUID groupId, UUID memberId);

    Optional<GroupShopMember> findMemberByUserId(UUID userId, UUID groupId);

    void payForItem(@NotNull UUID memberId, Integer price);

    UUID getAdminId(UUID groupId);
}
