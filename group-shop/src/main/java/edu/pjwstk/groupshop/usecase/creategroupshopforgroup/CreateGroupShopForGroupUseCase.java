package edu.pjwstk.groupshop.usecase.creategroupshopforgroup;

import edu.pjwstk.common.groupshopApi.dto.CreateGroupShopForGroupRequestDto;
import edu.pjwstk.common.groupshopApi.dto.CreateGroupShopForGroupResponseDto;

import java.util.UUID;

public interface CreateGroupShopForGroupUseCase {
    CreateGroupShopForGroupResponseDto execute(CreateGroupShopForGroupRequestDto request);

}
