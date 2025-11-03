package edu.pjwstk.groupshop.usecase.creategroupshopforgroup;

import edu.pjwstk.common.groupshopApi.dto.CreateGroupShopForGroupRequestDto;
import edu.pjwstk.common.groupshopApi.dto.CreateGroupShopForGroupResponseDto;
import edu.pjwstk.groupshop.usecase.creategroupshop.CreateGroupShopRequest;
import edu.pjwstk.groupshop.usecase.creategroupshop.CreateGroupShopResponse;

import java.util.UUID;

public interface CreateGroupShopForGroupUseCase {
    CreateGroupShopForGroupResponseDto execute(CreateGroupShopForGroupRequestDto request);

}
