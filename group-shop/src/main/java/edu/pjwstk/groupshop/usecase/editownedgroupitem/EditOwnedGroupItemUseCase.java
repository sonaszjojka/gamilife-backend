package edu.pjwstk.groupshop.usecase.editownedgroupitem;

import java.util.UUID;

public interface EditOwnedGroupItemUseCase {
    EditOwnedGroupItemResponse execute(EditOwnedGroupItemRequest request, UUID ownedGroupItemId, UUID groupMemberId, UUID groupId);
}
