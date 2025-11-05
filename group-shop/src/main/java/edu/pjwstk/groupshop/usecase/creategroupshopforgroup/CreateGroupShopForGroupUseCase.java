package edu.pjwstk.groupshop.usecase.creategroupshopforgroup;

import edu.pjwstk.api.groupshop.dto.CreateGroupShopForGroupRequestDto;
import edu.pjwstk.api.groupshop.dto.CreateGroupShopForGroupResponseDto;

public interface CreateGroupShopForGroupUseCase {
    CreateGroupShopForGroupResponseDto execute(CreateGroupShopForGroupRequestDto request);

}
