package edu.pjwstk.groupshop.usecase.creategroupshop;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.groupsApi.GroupApi;
import edu.pjwstk.common.groupsApi.dto.GroupDto;
import edu.pjwstk.groupshop.entity.GroupShop;
import edu.pjwstk.groupshop.exception.ShopForGroupAlreadyExistsException;
import edu.pjwstk.groupshop.exception.UserNotAdministratorException;
import edu.pjwstk.groupshop.repository.GroupShopRepository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class CreateGroupShopUseCaseImpl implements CreateGroupShopUseCase {
    private final CreateGroupShopMapper createGroupShopMapper;
    private final GroupShopRepository groupShopRepository;
    private final GroupApi groupProvider;
    private final AuthApi currentUserProvider;

    public CreateGroupShopUseCaseImpl(CreateGroupShopMapper createGroupShopMapper,
                                      GroupShopRepository groupShopRepository,
                                      GroupApi groupProvider, AuthApi currentUserProvider) {
        this.createGroupShopMapper = createGroupShopMapper;
        this.groupShopRepository = groupShopRepository;
        this.groupProvider = groupProvider;
        this.currentUserProvider = currentUserProvider;
    }

    @Override
    @Transactional
    public CreateGroupShopResponse execute(CreateGroupShopRequest request, UUID groupId) {
        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser().orElseThrow();
        GroupDto groupDto= groupProvider.findGroupById(groupId);

        if (!currentUserDto.userId().equals(groupDto.adminId())) {
            throw new UserNotAdministratorException("Only group administrators can create a group shop!");
        }

        if (groupShopRepository.findByGroupId(groupId).isPresent()) {
            throw new ShopForGroupAlreadyExistsException("Group shop for this group already exists");
        }

        GroupShop groupShop = createGroupShopMapper.toEntity(request,groupId,UUID.randomUUID());
        return createGroupShopMapper.toResponse(groupShopRepository.save(groupShop));
    }
}
