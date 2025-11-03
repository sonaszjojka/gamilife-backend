package edu.pjwstk.groupshop.usecase.changeGroupShopStatus;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.groupsApi.GroupApi;
import edu.pjwstk.common.groupsApi.dto.GroupDto;
import edu.pjwstk.groupshop.entity.GroupShop;
import edu.pjwstk.groupshop.exception.GroupShopNotFoundException;
import edu.pjwstk.groupshop.exception.UserNotAdministratorException;
import edu.pjwstk.groupshop.repository.GroupShopRepository;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class ChangeGroupShopStatusUseCaseImpl implements ChangeGroupShopStatusUseCase {
    GroupShopRepository groupShopRepository;
    ChangeGroupShopStatusMapper changeGroupShopStatusMapper;
    GroupApi groupApi;
    AuthApi authApi;

    public ChangeGroupShopStatusUseCaseImpl(GroupShopRepository groupShopRepository, ChangeGroupShopStatusMapper groupShopMapper, GroupApi groupApi, AuthApi authApi) {
        this.groupShopRepository = groupShopRepository;
        this.changeGroupShopStatusMapper = groupShopMapper;
        this.groupApi = groupApi;
        this.authApi = authApi;
    }

    @Override
    public ChangeGroupShopStatusResponse execute(ChangeGroupShopStatusRequest request, UUID shopId, UUID groupId) {

        CurrentUserDto currentUser = authApi.getCurrentUser().orElseThrow();
        GroupDto groupDto = groupApi.findGroupById(groupId);
        if (!currentUser.userId().equals(groupDto.adminId())) {
            throw new UserNotAdministratorException("Only group administrators can edit group shop!");
        }

        GroupShop groupShop = groupShopRepository.findByGroupShopId(shopId).orElseThrow(
                () -> new GroupShopNotFoundException("Group shop with id: " + shopId + " not found!"));

       groupShop.setIsActive(request.isActive());
        groupShopRepository.save(groupShop);

        return changeGroupShopStatusMapper.toResponse(groupShop);
    }
}
