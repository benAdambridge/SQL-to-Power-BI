-- WE ARE INTERESTED IN CUSTOMER PURCHASES
-- we created a view which looks at customers details (such as their company and where they are from)
-- and what shoes they bought, how many and at what price
-- (we do not create any calculation/ aggregate columns in this view as this means the data will not update when required)

CREATE VIEW customer_order_overview AS
SELECT c.company, c.city, c.local_authority, si.quantity, i.price, p.name, p.description
--, product.name, i.price, pt.name, si.quantity
FROM customer c
JOIN sales_order so ON so.cust_id = c.id
JOIN sales_item si ON si.sales_order_id = so.id
JOIN item i ON i.id = si.item_id
JOIN product p ON p.id = i.product_id;


-- the final product

select * from customer_order_overview

-- this view can now be uploaded into a visualization tool, to create a dashboard (such as Power BI)


