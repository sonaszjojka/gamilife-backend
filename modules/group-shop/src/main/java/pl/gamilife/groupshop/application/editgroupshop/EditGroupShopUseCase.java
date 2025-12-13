package pl.gamilife.groupshop.application.editgroupshop;

import java.util.UUID;

public interface EditGroupShopUseCase {
    EditGroupShopResponse execute(EditGroupShopRequest request, UUID shopId, UUID groupId);
}
