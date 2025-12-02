package pl.gamilife.groupshop.api;

import pl.gamilife.api.groupshop.GroupShopApi;
import pl.gamilife.api.groupshop.dto.CreateGroupShopForGroupRequestDto;
import pl.gamilife.api.groupshop.dto.CreateGroupShopForGroupResponseDto;
import pl.gamilife.groupshop.usecase.creategroupshopforgroup.CreateGroupShopForGroupUseCase;
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
