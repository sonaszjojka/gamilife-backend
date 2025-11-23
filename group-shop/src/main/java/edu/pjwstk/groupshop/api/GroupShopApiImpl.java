package edu.pjwstk.groupshop.api;

import edu.pjwstk.api.groupshop.GroupShopApi;
import edu.pjwstk.api.groupshop.dto.CreateGroupShopForGroupRequestDto;
import edu.pjwstk.api.groupshop.dto.CreateGroupShopForGroupResponseDto;
import edu.pjwstk.groupshop.usecase.creategroupshopforgroup.CreateGroupShopForGroupUseCase;
import org.springframework.stereotype.Service;

@Service
public class GroupShopApiImpl implements GroupShopApi {

    private final CreateGroupShopForGroupUseCase createGroupShopForGroupUseCase;

    public GroupShopApiImpl(CreateGroupShopForGroupUseCase createGroupShopForGroupUseCase) {
        this.createGroupShopForGroupUseCase = createGroupShopForGroupUseCase;
    }

    @Override
    public CreateGroupShopForGroupResponseDto createGroupShopOnGroupInit(CreateGroupShopForGroupRequestDto requestDto) {
        return createGroupShopForGroupUseCase.execute(requestDto);
    }
}
