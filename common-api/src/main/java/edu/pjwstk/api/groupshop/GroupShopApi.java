package edu.pjwstk.api.groupshop;

import edu.pjwstk.api.groupshop.dto.CreateGroupShopForGroupRequestDto;
import edu.pjwstk.api.groupshop.dto.CreateGroupShopForGroupResponseDto;

public interface GroupShopApi {
    CreateGroupShopForGroupResponseDto createGroupShopOnGroupInit(CreateGroupShopForGroupRequestDto requestDto);
}
