package edu.pjwstk.groupshop.usecase.editgroupshop;

import java.util.UUID;

public interface EditGroupShopUseCase {
    EditGroupShopResponse execute(EditGroupShopRequest request, UUID shopId, UUID groupId);
}
