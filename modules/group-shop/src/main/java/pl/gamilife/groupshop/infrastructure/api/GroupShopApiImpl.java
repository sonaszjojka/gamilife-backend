package pl.gamilife.groupshop.infrastructure.api;

import org.springframework.stereotype.Service;
import pl.gamilife.api.groupshop.GroupShopApi;
import pl.gamilife.api.groupshop.dto.CreateGroupShopForGroupRequestDto;
import pl.gamilife.api.groupshop.dto.CreateGroupShopForGroupResponseDto;
import pl.gamilife.groupshop.application.creategroupshopforgroup.CreateGroupShopForGroupCommand;
import pl.gamilife.groupshop.application.creategroupshopforgroup.CreateGroupShopForGroupResult;
import pl.gamilife.groupshop.application.creategroupshopforgroup.CreateGroupShopForGroupUseCase;

@Service
public class GroupShopApiImpl implements GroupShopApi {

    private final CreateGroupShopForGroupUseCase createGroupShopForGroupUseCase;

    public GroupShopApiImpl(CreateGroupShopForGroupUseCase createGroupShopForGroupUseCase) {
        this.createGroupShopForGroupUseCase = createGroupShopForGroupUseCase;
    }

    @Override
    public CreateGroupShopForGroupResponseDto createGroupShopOnGroupInit(CreateGroupShopForGroupRequestDto requestDto) {

        CreateGroupShopForGroupResult result = createGroupShopForGroupUseCase.execute(new CreateGroupShopForGroupCommand(
                requestDto.name(),
                requestDto.description(),
                requestDto.groupId()
        ));

        return new CreateGroupShopForGroupResponseDto(
                result.groupShopId(),
                result.name(),
                result.description()
        );
    }
}
