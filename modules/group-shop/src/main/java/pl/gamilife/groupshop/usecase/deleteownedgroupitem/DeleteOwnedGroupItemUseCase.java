package pl.gamilife.groupshop.usecase.deleteownedgroupitem;

import java.util.UUID;

public interface DeleteOwnedGroupItemUseCase {

    void execute(UUID groupId, UUID memberId, UUID ownedGroupItemId);
}
