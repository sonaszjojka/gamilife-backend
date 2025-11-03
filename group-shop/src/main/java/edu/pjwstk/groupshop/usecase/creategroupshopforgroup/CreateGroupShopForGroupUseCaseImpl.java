package edu.pjwstk.groupshop.usecase.creategroupshopforgroup;

import edu.pjwstk.common.groupshopApi.dto.CreateGroupShopForGroupRequestDto;
import edu.pjwstk.common.groupshopApi.dto.CreateGroupShopForGroupResponseDto;
import edu.pjwstk.groupshop.entity.GroupShop;
import edu.pjwstk.groupshop.repository.GroupShopRepository;
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

        GroupShop groupShop = createGroupShopForGroupMapper.toEntity(request,UUID.randomUUID());
        groupShopRepository.save(groupShop);

        return createGroupShopForGroupMapper.toResponse(groupShop);
    }
}
