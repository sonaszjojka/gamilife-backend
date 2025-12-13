package pl.gamilife.groupshop.application.createownedgroupitem;

import java.util.UUID;

public interface CreateOwnedGroupItemUseCase {

    CreateOwnedGroupItemResponse execute(CreateOwnedGroupItemRequest request, UUID groupMemberId, UUID groupId);
}
