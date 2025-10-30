package edu.pjwstk.common.groupshopApi;

import edu.pjwstk.common.groupshopApi.dto.CreateGroupShopForGroupRequestDto;
import edu.pjwstk.common.groupshopApi.dto.CreateGroupShopForGroupResponseDto;

public interface GroupShopApi {
    CreateGroupShopForGroupResponseDto createGroupShopOnGroupInit(CreateGroupShopForGroupRequestDto requestDto);
}
