package pl.gamilife.gamification.domain.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import pl.gamilife.gamification.domain.exception.UserDoesNotHaveEnoughItemsException;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.util.UUID;

@Getter
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@ToString(exclude = {"item"})
@Table(name = "user_inventory_item", schema = "gamification")
public class UserInventoryItem extends BaseEntity {

    @Column(name = "item_id", nullable = false, insertable = false, updatable = false)
    private UUID itemId;

    @NotNull
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "item_id", nullable = false)
    private Item item;

    @NotNull
    @Column(name = "user_id", nullable = false)
    private UUID userId;

    @NotNull
    @Column(name = "quantity", nullable = false)
    private Integer quantity;

    @NotNull
    @Column(name = "is_equipped", nullable = false)
    private Boolean isEquipped = false;

    private UserInventoryItem(UUID userId, Item item, Integer quantity) {
        setUserId(userId);
        setItem(item);
        setQuantity(quantity);
    }

    public static UserInventoryItem create(UUID userId, Item item, Integer quantity) {
        return new UserInventoryItem(userId, item, quantity);
    }

    private void setUserId(UUID userId) {
        if (userId == null) {
            throw new IllegalArgumentException("User ID cannot be null");
        }

        this.userId = userId;
    }

    private void setItem(Item item) {
        if (item == null) {
            throw new IllegalArgumentException("Item cannot be null");
        }

        this.item = item;
        this.itemId = item.getId();
    }

    private void setQuantity(Integer quantity) {
        if (quantity == null) {
            throw new IllegalArgumentException("Quantity cannot be null");
        }

        this.quantity = quantity;
    }

    public boolean doesBelongTo(UUID userId) {
        return this.userId.equals(userId);
    }

    public void incrementQuantityBy(int amount) {
        if (amount <= 0) {
            throw new IllegalArgumentException("Quantity increment be greater than 0");
        }

        this.quantity += amount;
    }

    public int quickSellItems(Integer amount) {
        if (amount > quantity) {
            throw new UserDoesNotHaveEnoughItemsException(
                    String.format("User has only %s such items in inventory", quantity)
            );
        }

        quantity -= amount;

        return item.getQuickSellValue() * amount;
    }

    public boolean changeEquippedStatus(boolean newStatus) {
        if (isEquipped == newStatus) {
            return false;
        }

        this.isEquipped = newStatus;
        return true;
    }
}