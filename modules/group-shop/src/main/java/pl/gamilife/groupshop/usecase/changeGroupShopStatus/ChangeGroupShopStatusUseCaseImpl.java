package pl.gamilife.groupshop.usecase.changeGroupShopStatus;

import org.springframework.stereotype.Service;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.api.group.dto.GroupDto;
import pl.gamilife.groupshop.entity.GroupShop;
import pl.gamilife.groupshop.exception.domain.GroupShopNotFoundException;
import pl.gamilife.groupshop.repository.GroupShopRepository;
import pl.gamilife.infrastructure.core.exception.domain.GroupAdminPrivilegesRequiredException;

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

        CurrentUserDto currentUser = authApi.getCurrentUser();
        GroupDto groupDto = groupApi.findGroupById(groupId);
        if (!currentUser.userId().equals(groupDto.adminId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can edit group shop!");
        }

        GroupShop groupShop = groupShopRepository.findByGroupShopId(shopId).orElseThrow(
                () -> new GroupShopNotFoundException("Group shop with id: " + shopId + " not found!"));

        groupShop.setIsActive(request.isActive());
        groupShopRepository.save(groupShop);

        return changeGroupShopStatusMapper.toResponse(groupShop);
    }
}
