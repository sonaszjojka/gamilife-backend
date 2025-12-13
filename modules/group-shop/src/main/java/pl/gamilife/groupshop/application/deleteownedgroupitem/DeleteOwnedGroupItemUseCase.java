package pl.gamilife.groupshop.application.deleteownedgroupitem;

import java.util.UUID;

public interface DeleteOwnedGroupItemUseCase {

    void execute(UUID groupId, UUID memberId, UUID ownedGroupItemId);
}
