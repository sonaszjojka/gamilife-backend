package pl.gamilife.groupshop.infrastructure.web;

import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.groupshop.application.changegroupshopstatus.ChangeGroupStatusCommand;
import pl.gamilife.groupshop.application.changegroupshopstatus.ChangeGroupShopStatusResult;
import pl.gamilife.groupshop.application.changegroupshopstatus.ChangeGroupShopStatusUseCase;
import pl.gamilife.groupshop.application.creategroupshopforgroup.CreateGroupShopForGroupCommand;
import pl.gamilife.groupshop.application.editgroupshop.EditGroupShopCommand;
import pl.gamilife.groupshop.infrastructure.web.request.ChangeGroupShopStatusRequest;
import pl.gamilife.groupshop.infrastructure.web.request.EditGroupShopRequest;
import pl.gamilife.groupshop.application.editgroupshop.EditGroupShopResult;
import pl.gamilife.groupshop.application.editgroupshop.EditGroupShopUseCase;
import pl.gamilife.shared.web.security.annotation.CurrentUserId;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/shop")
public class GroupShopController {
    private final ChangeGroupShopStatusUseCase changeGroupShopStatusUseCase;
    private final EditGroupShopUseCase editGroupShopUseCase;


    public GroupShopController(ChangeGroupShopStatusUseCase changeGroupShopStatusUseCase, EditGroupShopUseCase editGroupShopUseCase) {
        this.changeGroupShopStatusUseCase = changeGroupShopStatusUseCase;
        this.editGroupShopUseCase = editGroupShopUseCase;
    }


    @PutMapping("/{shopId}")
    public ResponseEntity<EditGroupShopResult> editGroupShop(@PathVariable(name = "groupId") UUID groupId,
                                                             @PathVariable(name = "shopId") UUID shopId,
                                                             @CurrentUserId UUID currentUserId,
                                                             @RequestBody EditGroupShopRequest request) {

        EditGroupShopCommand cmd = new EditGroupShopCommand(request.name(),request.description(),groupId,shopId,currentUserId);

        EditGroupShopResult response = editGroupShopUseCase.execute(cmd);
        return ResponseEntity.ok(response);
    }

    @PutMapping("/{shopId}/status")
    public ResponseEntity<ChangeGroupShopStatusResult> changeGroupShopStatus(@PathVariable(name = "groupId") UUID groupId,
                                                                             @PathVariable(name = "shopId") UUID shopId,
                                                                             @CurrentUserId UUID currentUserId,
                                                                             @RequestBody @Valid ChangeGroupShopStatusRequest request) {
        ChangeGroupStatusCommand cmd= new ChangeGroupStatusCommand(request.isActive(),shopId,groupId,currentUserId);
        ChangeGroupShopStatusResult response = changeGroupShopStatusUseCase.execute(cmd);
        return ResponseEntity.ok(response);
    }
}
