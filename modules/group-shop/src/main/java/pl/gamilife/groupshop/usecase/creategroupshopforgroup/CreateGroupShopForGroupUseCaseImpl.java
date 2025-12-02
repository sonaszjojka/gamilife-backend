package pl.gamilife.groupshop.usecase.creategroupshopforgroup;

import pl.gamilife.api.groupshop.dto.CreateGroupShopForGroupRequestDto;
import pl.gamilife.api.groupshop.dto.CreateGroupShopForGroupResponseDto;
import pl.gamilife.groupshop.entity.GroupShop;
import pl.gamilife.groupshop.repository.GroupShopRepository;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class CreateGroupShopForGroupUseCaseImpl implements CreateGroupShopForGroupUseCase {
    private final CreateGroupShopForGroupMapper createGroupShopForGroupMapper;
    private final GroupShopRepository groupShopRepository;

    public CreateGroupShopForGroupUseCaseImpl(CreateGroupShopForGroupMapper createGroupShopForGroupMapper, GroupShopRepository groupShopRepository) {
        this.createGroupShopForGroupMapper = createGroupShopForGroupMapper;
        this.groupShopRepository = groupShopRepository;
    }

    @Override
    public CreateGroupShopForGroupResponseDto execute(CreateGroupShopForGroupRequestDto request) {

        GroupShop groupShop = createGroupShopForGroupMapper.toEntity(request, UUID.randomUUID());
        groupShopRepository.save(groupShop);

        return createGroupShopForGroupMapper.toResponse(groupShop);
    }
}
