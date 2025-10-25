package edu.pjwstk.groupshop.usecase.createownedgroupitem;

import java.util.UUID;

public interface CreateOwnedGroupItemUseCase {

    CreateOwnedGroupItemResponse execute(CreateOwnedGroupItemRequest request, UUID groupMemberId,UUID groupId);
}
