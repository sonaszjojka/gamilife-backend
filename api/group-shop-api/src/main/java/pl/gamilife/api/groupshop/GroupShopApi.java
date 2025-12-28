package pl.gamilife.api.groupshop;

import pl.gamilife.api.groupshop.dto.CreateGroupShopForGroupRequestDto;
import pl.gamilife.api.groupshop.dto.CreateGroupShopForGroupResponseDto;

public interface GroupShopApi {
    CreateGroupShopForGroupResponseDto createGroupShopOnGroupInit(CreateGroupShopForGroupRequestDto requestDto);
}
