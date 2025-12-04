package pl.gamilife.groupshop.usecase.editgroupshop;

import java.util.UUID;

public interface EditGroupShopUseCase {
    EditGroupShopResponse execute(EditGroupShopRequest request, UUID shopId, UUID groupId);
}
