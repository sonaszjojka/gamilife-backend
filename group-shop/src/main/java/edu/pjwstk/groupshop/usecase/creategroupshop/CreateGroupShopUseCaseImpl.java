package edu.pjwstk.groupshop.usecase.creategroupshop;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.groupsApi.GroupApi;
import edu.pjwstk.groupshop.entity.GroupShop;
import edu.pjwstk.groupshop.exception.ShopForGroupAlreadyExistsException;
import edu.pjwstk.groupshop.repository.GroupShopRepository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class CreateGroupShopUseCaseImpl implements CreateGroupShopUseCase {
    private final CreateGroupShopMapper createGroupShopMapper;
    private final GroupShopRepository groupShopRepository;
    private final GroupApi groupApi;

    public CreateGroupShopUseCaseImpl(CreateGroupShopMapper createGroupShopMapper,
                                      GroupShopRepository groupShopRepository,
                                      GroupApi groupApi) {
        this.createGroupShopMapper = createGroupShopMapper;
        this.groupShopRepository = groupShopRepository;
        this.groupApi = groupApi;
    }

    @Override
    @Transactional
    public CreateGroupShopResponse execute(CreateGroupShopRequest request, UUID groupId) {

        groupApi.findGroupById(groupId);

        if (groupShopRepository.findByGroupId(groupId).isPresent()) {
            throw new ShopForGroupAlreadyExistsException("Group shop for this group already exists");
        }

        GroupShop groupShop = createGroupShopMapper.toEntity(request,groupId,UUID.randomUUID());
        return createGroupShopMapper.toResponse(groupShopRepository.save(groupShop));
    }
}
