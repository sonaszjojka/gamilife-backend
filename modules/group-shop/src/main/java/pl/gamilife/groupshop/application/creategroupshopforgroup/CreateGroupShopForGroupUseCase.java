package pl.gamilife.groupshop.application.creategroupshopforgroup;

import pl.gamilife.api.groupshop.dto.CreateGroupShopForGroupRequestDto;
import pl.gamilife.api.groupshop.dto.CreateGroupShopForGroupResponseDto;

public interface CreateGroupShopForGroupUseCase {
    CreateGroupShopForGroupResponseDto execute(CreateGroupShopForGroupRequestDto request);

}
