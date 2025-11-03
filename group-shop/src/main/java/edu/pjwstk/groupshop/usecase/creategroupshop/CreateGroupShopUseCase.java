package edu.pjwstk.groupshop.usecase.creategroupshop;

import java.util.UUID;

public interface CreateGroupShopUseCase {
    CreateGroupShopResponse execute(CreateGroupShopRequest request, UUID groupId);

}
